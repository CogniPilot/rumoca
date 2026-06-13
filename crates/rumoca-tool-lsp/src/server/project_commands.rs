use super::*;
use std::path::{Path, PathBuf};

use anyhow::Context;
use rumoca_compile::codegen::render_dae_template_with_name;
use rumoca_compile::codegen::targets::{
    RenderedTargetFile, TargetBundle, TargetTemplateIr, builtin_target_descriptors_for_ir,
    render_dae_target_files, target_ir_is_dae_renderable,
};

fn builtin_template_descriptors() -> Vec<rumoca_compile::codegen::targets::BuiltinTargetDescriptor>
{
    builtin_target_descriptors_for_ir(TargetTemplateIr::Dae)
}

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
                    format!("[rumoca] colocated model config: {diagnostic}"),
                )
                .await;
        }
    }

    pub(super) async fn execute_get_builtin_targets(&self) -> Option<Value> {
        serde_json::to_value(builtin_template_descriptors()).ok()
    }

    pub(super) async fn execute_get_scenario_config(&self, params: Option<Value>) -> Option<Value> {
        let params_value = params?;
        let obj = params_value.as_object()?;
        let uri = obj.get("uri").and_then(Value::as_str)?;
        let uri = Url::parse(uri).ok()?;
        if !is_project_config_uri(&uri) {
            return Some(Self::simulation_error_value(
                "scenario config must be a rum.toml or rum.<profile>.toml file",
            ));
        }
        let source = match self.open_document_source_for_uri(&uri).await {
            Ok(source) => source,
            Err(error) => return Some(Self::simulation_error_value(error)),
        };
        Some(scenario_config_response(&source))
    }

    pub(super) async fn execute_render_target(&self, params: Option<Value>) -> Option<Value> {
        let params_value = params?;
        let obj = params_value.as_object()?;
        let uri = obj.get("uri").and_then(Value::as_str)?;
        let uri = Url::parse(uri).ok()?;
        let model = obj.get("model").and_then(Value::as_str)?.trim().to_string();
        let target_name = obj
            .get("target")
            .and_then(Value::as_str)?
            .trim()
            .to_string();
        if model.is_empty() {
            return Some(Self::simulation_error_value("model is required"));
        }
        if target_name.is_empty() {
            return Some(Self::simulation_error_value("target is required"));
        }

        let mut request_token = self.begin_analysis_request().await;
        let (_focus_uri, uri_path, source) = match self.simulation_focus_for_uri(&uri).await {
            Ok(focus) => focus,
            Err(error) => return Some(Self::simulation_error_value(error)),
        };
        let target_base_path = render_target_base_path(&uri, Path::new(&uri_path));

        if self
            .wait_for_simulation_prewarm_if_current(&model, &uri_path)
            .await
        {
            request_token = self.refresh_analysis_request_revision(request_token).await;
        }

        let source_root_paths = self.source_root_paths.read().await.clone();
        let loaded_source_roots = self
            .ensure_source_roots_loaded_with_paths(&source, &uri_path, &source_root_paths)
            .await;
        if loaded_source_roots {
            request_token = self.refresh_analysis_request_revision(request_token).await;
        }
        if let Some(response) = self.stale_simulation_response(request_token).await {
            return Some(response);
        }

        let _strict_lane = self.work_lanes.strict.lock().await;
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

        let target_path = resolve_scenario_codegen_target(&target_base_path, &target_name);
        if raw_jinja_target(&target_path) {
            return match render_raw_jinja_target(compiled.dae.as_ref(), &model, &target_path) {
                Ok(files) => Some(json!({
                    "ok": true,
                    "target": target_name,
                    "files": files,
                })),
                Err(error) => Some(Self::simulation_error_value(format!(
                    "target render failed: {error}",
                ))),
            };
        }

        let bundle = match load_codegen_target_bundle(&target_name, &target_path) {
            Ok(bundle) => bundle,
            Err(error) => return Some(Self::simulation_error_value(error.to_string())),
        };
        let manifest = match bundle.parse_manifest() {
            Ok(manifest) => manifest,
            Err(error) => return Some(Self::simulation_error_value(error.to_string())),
        };
        if !target_ir_is_dae_renderable(manifest.ir) {
            return Some(Self::simulation_error_value(format!(
                "target '{}' uses {:?} IR; editor target rendering currently supports dae IR targets",
                target_name, manifest.ir
            )));
        }

        match render_dae_target_files(&bundle, &manifest, compiled.dae.as_ref(), &model) {
            Ok(files) => Some(json!({
                "ok": true,
                "target": target_name,
                "files": files,
            })),
            Err(error) => Some(Self::simulation_error_value(format!(
                "target render failed: {error}",
            ))),
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
        Some(visualization_views_to_json(views))
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
}

fn load_codegen_target_bundle(
    target_name: &str,
    target_path: &Path,
) -> anyhow::Result<TargetBundle> {
    if let Some(bundle) = TargetBundle::builtin(target_name) {
        return Ok(bundle);
    }
    TargetBundle::load(
        target_path
            .to_str()
            .ok_or_else(|| anyhow::anyhow!("codegen target path is not UTF-8"))?,
    )
}

fn resolve_scenario_codegen_target(uri_path: &Path, target: &str) -> PathBuf {
    let target_path = PathBuf::from(target);
    if target_path.is_absolute() {
        return target_path;
    }
    uri_path
        .parent()
        .unwrap_or_else(|| Path::new(""))
        .join(target_path)
}

fn render_target_base_path(request_uri: &Url, focus_path: &Path) -> PathBuf {
    if is_project_config_uri(request_uri)
        && let Ok(config_path) = request_uri.to_file_path()
    {
        return config_path;
    }
    focus_path.to_path_buf()
}

fn raw_jinja_target(target_path: &Path) -> bool {
    target_path
        .extension()
        .and_then(|extension| extension.to_str())
        .is_some_and(|extension| extension == "jinja")
}

fn render_raw_jinja_target(
    dae: &rumoca_compile::compile::Dae,
    model: &str,
    target_path: &Path,
) -> anyhow::Result<Vec<RenderedTargetFile>> {
    let template = std::fs::read_to_string(target_path)
        .with_context(|| format!("Read template: {}", target_path.display()))?;
    let model_identifier = model.replace('.', "_");
    let content = render_dae_template_with_name(dae, &template, &model_identifier)
        .with_context(|| format!("Render raw template: {}", target_path.display()))?;
    Ok(vec![RenderedTargetFile {
        path: raw_jinja_output_path(target_path),
        content,
    }])
}

fn raw_jinja_output_path(target_path: &Path) -> String {
    target_path
        .file_stem()
        .and_then(|stem| stem.to_str())
        .filter(|stem| !stem.is_empty())
        .unwrap_or("rendered")
        .to_string()
}
