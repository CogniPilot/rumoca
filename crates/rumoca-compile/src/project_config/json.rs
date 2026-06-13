//! JSON shaping for project (`rum.toml`) configuration commands.
//!
//! These pure converters between the `rum.toml` config types and the camelCase
//! JSON the editors exchange are the single source of truth shared by the LSP
//! server (`rumoca-tool-lsp`) and the browser editor bindings
//! (`rumoca-bind-wasm`), so the two cannot drift.

use serde_json::{Value, json};

use super::{
    EffectiveSimulationConfig, EffectiveSimulationPreset, PlotViewConfig, ProjectConfigFile,
    ProjectTask, ScenarioViewerMode, SimulationModelOverride, normalize_dt_opt,
    normalize_solver_opt,
};

/// `EffectiveSimulationConfig` -> simulation settings JSON (camelCase).
pub fn simulation_settings_to_json(settings: &EffectiveSimulationConfig) -> Value {
    json!({
        "solver": settings.solver,
        "tEnd": settings.t_end,
        "dt": settings.dt,
        "outputDir": settings.output_dir,
        "sourceRootPaths": settings.source_root_paths,
    })
}

/// `EffectiveSimulationPreset` -> preset JSON (camelCase).
pub fn simulation_preset_to_json(preset: &EffectiveSimulationPreset) -> Value {
    json!({
        "solver": preset.solver,
        "tEnd": preset.t_end,
        "dt": preset.dt,
        "outputDir": preset.output_dir,
        "sourceRootOverrides": preset.source_root_overrides,
    })
}

/// Parse the optional `fallback` simulation settings an editor supplies when
/// asking for a model's effective config.
pub fn parse_fallback_simulation(value: Option<&Value>) -> Option<EffectiveSimulationConfig> {
    let obj = value?.as_object()?;
    let solver = normalize_solver_opt(
        obj.get("solver")
            .and_then(Value::as_str)
            .map(str::to_string),
    )
    .unwrap_or_else(|| "auto".to_string());
    let t_end = obj
        .get("tEnd")
        .and_then(Value::as_f64)
        .filter(|v| v.is_finite() && *v > 0.0)
        .unwrap_or(10.0);
    let dt = normalize_dt_opt(obj.get("dt").and_then(Value::as_f64));
    let output_dir = obj
        .get("outputDir")
        .and_then(Value::as_str)
        .unwrap_or_default()
        .to_string();
    let source_root_paths = string_array(obj.get("sourceRootPaths"));
    Some(EffectiveSimulationConfig {
        solver,
        t_end,
        dt,
        output_dir,
        source_root_paths,
    })
}

/// Parse a simulation preset (`setSimulationPreset` payload) into a model
/// override.
pub fn simulation_override_from_json(value: &Value) -> Option<SimulationModelOverride> {
    let obj = value.as_object()?;
    Some(SimulationModelOverride {
        solver: obj
            .get("solver")
            .and_then(Value::as_str)
            .map(str::to_string),
        t_end: obj.get("tEnd").and_then(Value::as_f64),
        dt: obj.get("dt").and_then(Value::as_f64),
        output_dir: obj
            .get("outputDir")
            .and_then(Value::as_str)
            .map(str::to_string),
        source_root_overrides: string_array(obj.get("sourceRootOverrides")),
    })
}

/// Configured plot views -> `{ "views": [...] }` JSON (camelCase).
pub fn visualization_views_to_json(views: Vec<PlotViewConfig>) -> Value {
    let payload: Vec<VisualizationViewPayload> = views
        .into_iter()
        .map(VisualizationViewPayload::from)
        .collect();
    json!({ "views": payload })
}

/// Parse a `setVisualizationConfig` views payload into plot view configs.
pub fn parse_views_payload(value: &Value) -> Option<Vec<PlotViewConfig>> {
    serde_json::from_value::<Vec<VisualizationViewPayload>>(value.clone())
        .ok()
        .map(|views| views.into_iter().map(PlotViewConfig::from).collect())
}

/// Build the `getScenarioConfig` response for a `rum.toml` file's text. Returns
/// `{ ok: false, error }` for any malformed/incomplete scenario.
pub fn scenario_config_response(content: &str) -> Value {
    let raw_config = match toml::from_str::<toml::Value>(content) {
        Ok(config) => config,
        Err(error) => return scenario_error(format!("failed to parse scenario config: {error}")),
    };
    let config = match toml::from_str::<ProjectConfigFile>(content) {
        Ok(config) => config,
        Err(error) => return scenario_error(format!("failed to parse scenario config: {error}")),
    };
    let model = config
        .model
        .name
        .as_deref()
        .map(str::trim)
        .filter(|value| !value.is_empty())
        .unwrap_or("");
    if model.is_empty() {
        return scenario_error("scenario config is missing [model].name");
    }
    let target = config.codegen.target.as_deref().unwrap_or("").trim();
    if config.rumoca.task == ProjectTask::Codegen && target.is_empty() {
        return scenario_error("codegen scenario config is missing [codegen].target");
    }
    json!({
        "ok": true,
        "task": match config.rumoca.task {
            ProjectTask::Simulate => "simulate",
            ProjectTask::Codegen => "codegen",
        },
        "model": model,
        "target": target,
        "outputDir": config.codegen.output_dir,
        "viewerMode": scenario_viewer_mode(&config, &raw_config),
        "viewerPreferExternal": config.viewer.prefer_external,
        "simMode": scenario_sim_mode(&raw_config),
        "httpPort": scenario_http_port(&raw_config),
        "websocketPort": scenario_websocket_port(&raw_config),
        "httpScene": scenario_http_scene(&raw_config),
        "interactive": scenario_requests_live_viewer(&raw_config),
    })
}

fn scenario_error(message: impl Into<String>) -> Value {
    json!({ "ok": false, "error": message.into() })
}

fn string_array(value: Option<&Value>) -> Vec<String> {
    value
        .and_then(Value::as_array)
        .map(|items| {
            items
                .iter()
                .filter_map(Value::as_str)
                .map(str::trim)
                .filter(|v| !v.is_empty())
                .map(str::to_string)
                .collect()
        })
        .unwrap_or_default()
}

fn table_has_key(value: &toml::Value, key: &str) -> bool {
    value
        .as_table()
        .is_some_and(|table| table.contains_key(key))
}

fn table_path<'a>(value: &'a toml::Value, path: &[&str]) -> Option<&'a toml::Value> {
    let mut current = value;
    for key in path {
        current = current.as_table()?.get(*key)?;
    }
    Some(current)
}

fn scenario_requests_live_viewer(raw: &toml::Value) -> bool {
    table_path(raw, &["transport", "http"]).is_some()
        || table_has_key(raw, "input")
        || table_has_key(raw, "signals")
        || table_has_key(raw, "locals")
        || table_has_key(raw, "derive")
        || table_has_key(raw, "reset")
        || table_has_key(raw, "external_interface")
        || (table_has_key(raw, "schema")
            && table_has_key(raw, "receive")
            && table_has_key(raw, "send"))
}

fn scenario_viewer_mode(config: &ProjectConfigFile, raw: &toml::Value) -> &'static str {
    match config.viewer.mode {
        Some(ScenarioViewerMode::ResultsPanel) => "results_panel",
        Some(ScenarioViewerMode::ExternalWeb) => "external_web",
        None if scenario_requests_live_viewer(raw) => "external_web",
        None => "results_panel",
    }
}

fn scenario_http_port(raw: &toml::Value) -> Option<i64> {
    table_path(raw, &["transport", "http", "port"]).and_then(toml::Value::as_integer)
}

fn scenario_websocket_port(raw: &toml::Value) -> Option<i64> {
    table_path(raw, &["transport", "websocket", "port"]).and_then(toml::Value::as_integer)
}

fn scenario_http_scene(raw: &toml::Value) -> Option<&str> {
    table_path(raw, &["transport", "http", "scene"]).and_then(toml::Value::as_str)
}

fn scenario_sim_mode(raw: &toml::Value) -> Option<&str> {
    table_path(raw, &["sim", "mode"]).and_then(toml::Value::as_str)
}

/// camelCase wire representation of a plot view, shared with the editors.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
struct VisualizationViewPayload {
    id: String,
    title: String,
    #[serde(rename = "type")]
    view_type: String,
    x: Option<String>,
    #[serde(default)]
    y: Vec<String>,
    script: Option<String>,
    script_path: Option<String>,
}

impl From<PlotViewConfig> for VisualizationViewPayload {
    fn from(view: PlotViewConfig) -> Self {
        Self {
            id: view.id,
            title: view.title,
            view_type: view.view_type,
            x: view.x,
            y: view.y,
            script: view.script,
            script_path: view.script_path,
        }
    }
}

impl From<VisualizationViewPayload> for PlotViewConfig {
    fn from(view: VisualizationViewPayload) -> Self {
        Self {
            id: view.id,
            title: view.title,
            view_type: view.view_type,
            x: view.x,
            y: view.y,
            script: view.script,
            script_path: view.script_path,
        }
    }
}
