use super::*;
use rumoca_session::runtime::{render_dae_template_with_name, templates as runtime_templates};
use serde::Serialize;

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
struct BuiltinTemplateDescriptor {
    id: &'static str,
    label: &'static str,
    language: &'static str,
    source: &'static str,
}

fn builtin_template_descriptors() -> Vec<BuiltinTemplateDescriptor> {
    vec![
        BuiltinTemplateDescriptor {
            id: "sympy.py.jinja",
            label: "SymPy (Python)",
            language: "python",
            source: runtime_templates::SYMPY,
        },
        BuiltinTemplateDescriptor {
            id: "jax.py.jinja",
            label: "JAX / Diffrax (Python)",
            language: "python",
            source: runtime_templates::JAX,
        },
        BuiltinTemplateDescriptor {
            id: "onnx.py.jinja",
            label: "ONNX (Python)",
            language: "python",
            source: runtime_templates::ONNX,
        },
        BuiltinTemplateDescriptor {
            id: "julia_mtk.jl.jinja",
            label: "Julia MTK",
            language: "julia",
            source: runtime_templates::JULIA_MTK,
        },
        BuiltinTemplateDescriptor {
            id: "casadi_sx.py.jinja",
            label: "CasADi SX (Python)",
            language: "python",
            source: runtime_templates::CASADI_SX,
        },
        BuiltinTemplateDescriptor {
            id: "casadi_mx.py.jinja",
            label: "CasADi MX (Python)",
            language: "python",
            source: runtime_templates::CASADI_MX,
        },
        BuiltinTemplateDescriptor {
            id: "embedded_c.h.jinja",
            label: "Embedded C Header",
            language: "c",
            source: runtime_templates::EMBEDDED_C_H,
        },
        BuiltinTemplateDescriptor {
            id: "embedded_c_impl.c.jinja",
            label: "Embedded C Implementation",
            language: "c",
            source: runtime_templates::EMBEDDED_C_IMPL,
        },
        BuiltinTemplateDescriptor {
            id: "dae_modelica.mo.jinja",
            label: "DAE Modelica",
            language: "modelica",
            source: runtime_templates::DAE_MODELICA,
        },
        BuiltinTemplateDescriptor {
            id: "flat_modelica.mo.jinja",
            label: "Flat Modelica",
            language: "modelica",
            source: runtime_templates::FLAT_MODELICA,
        },
        BuiltinTemplateDescriptor {
            id: "fmi2_model_description.xml.jinja",
            label: "FMI 2.0 modelDescription.xml",
            language: "xml",
            source: runtime_templates::FMI2_MODEL_DESCRIPTION,
        },
        BuiltinTemplateDescriptor {
            id: "fmi2_model.c.jinja",
            label: "FMI 2.0 model.c",
            language: "c",
            source: runtime_templates::FMI2_MODEL,
        },
        BuiltinTemplateDescriptor {
            id: "fmi2_test_driver.c.jinja",
            label: "FMI 2.0 test driver",
            language: "c",
            source: runtime_templates::FMI2_TEST_DRIVER,
        },
        BuiltinTemplateDescriptor {
            id: "fmi3_model_description.xml.jinja",
            label: "FMI 3.0 modelDescription.xml",
            language: "xml",
            source: runtime_templates::FMI3_MODEL_DESCRIPTION,
        },
        BuiltinTemplateDescriptor {
            id: "fmi3_model.c.jinja",
            label: "FMI 3.0 model.c",
            language: "c",
            source: runtime_templates::FMI3_MODEL,
        },
        BuiltinTemplateDescriptor {
            id: "fmi3_test_driver.c.jinja",
            label: "FMI 3.0 test driver",
            language: "c",
            source: runtime_templates::FMI3_TEST_DRIVER,
        },
    ]
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
                    format!("[rumoca] project config: {diagnostic}"),
                )
                .await;
        }
    }

    pub(super) async fn execute_get_builtin_templates(&self) -> Option<Value> {
        serde_json::to_value(builtin_template_descriptors()).ok()
    }

    pub(super) async fn execute_render_template(&self, params: Option<Value>) -> Option<Value> {
        let params_value = params?;
        let obj = params_value.as_object()?;
        let uri = obj.get("uri").and_then(Value::as_str)?;
        let uri = Url::parse(uri).ok()?;
        let model = obj.get("model").and_then(Value::as_str)?.trim().to_string();
        let template = obj.get("template").and_then(Value::as_str)?.to_string();
        if model.is_empty() {
            return Some(Self::simulation_error_value("model is required"));
        }
        if template.trim().is_empty() {
            return Some(Self::simulation_error_value("template source is required"));
        }

        let mut request_token = self.begin_analysis_request().await;
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

        match render_dae_template_with_name(compiled.dae.as_ref(), &template, &model) {
            Ok(output) => Some(json!({
                "ok": true,
                "output": output,
            })),
            Err(error) => Some(Self::simulation_error_value(format!(
                "template render failed: {error}",
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
