//! JSON shaping for project (`rumoca-scenario.toml`) configuration commands.
//!
//! These pure converters between the `rumoca-scenario.toml` config types and the camelCase
//! JSON the editors exchange are the single source of truth shared by the LSP
//! server (`rumoca-tool-lsp`) and the browser editor bindings
//! (`rumoca-bind-wasm`), so the two cannot drift.

use serde_json::{Value, json};

use super::{
    CodegenConfig, EffectiveSimulationConfig, EffectiveSimulationPreset, PlotViewConfig,
    ScenarioConfigFile, ScenarioTask, ScenarioViewerMode, SimulationModelOverride,
    normalize_dt_opt, normalize_solver_opt,
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

/// `[codegen]` target settings -> editor JSON (camelCase).
pub fn codegen_config_to_json(config: &CodegenConfig) -> Value {
    json!({
        "target": config.target,
        "outputDir": config.output_dir,
    })
}

/// Parse editor JSON into a `[codegen]` config.
pub fn codegen_config_from_json(value: &Value) -> Option<CodegenConfig> {
    let obj = value.as_object()?;
    Some(CodegenConfig {
        target: obj
            .get("target")
            .and_then(Value::as_str)
            .map(str::to_string),
        output_dir: obj
            .get("outputDir")
            .and_then(Value::as_str)
            .map(str::to_string),
    })
}

/// Scenario-local source roots -> editor JSON (camelCase).
pub fn source_roots_to_json(paths: &[String]) -> Value {
    json!({ "sourceRootPaths": paths })
}

/// Parse editor JSON into scenario-local source roots.
pub fn source_roots_from_json(value: &Value) -> Option<Vec<String>> {
    let obj = value.as_object()?;
    Some(string_array(obj.get("sourceRootPaths")))
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

/// Build the `getScenarioConfig` response for a `rumoca-scenario.toml` file's text. Returns
/// `{ ok: false, error }` for any malformed/incomplete scenario.
pub fn scenario_config_response(content: &str) -> Value {
    let raw_config = match toml::from_str::<toml::Value>(content) {
        Ok(config) => config,
        Err(error) => return scenario_error(format!("failed to parse scenario config: {error}")),
    };
    let config = match toml::from_str::<ScenarioConfigFile>(content) {
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
    if config.rumoca.task == ScenarioTask::Codegen && target.is_empty() {
        return scenario_error("codegen scenario config is missing [codegen].target");
    }
    json!({
        "ok": true,
        "task": match config.rumoca.task {
            ScenarioTask::Simulate => "simulate",
            ScenarioTask::Codegen => "codegen",
        },
        "model": model,
        "target": target,
        "outputDir": config.codegen.output_dir,
        "viewerMode": scenario_viewer_mode(&config),
        "viewerPreferExternal": config.viewer.prefer_external,
        "simMode": scenario_sim_mode(&raw_config),
        "httpPort": scenario_http_port(&raw_config),
        "websocketPort": scenario_websocket_port(&raw_config),
        "httpScene": scenario_http_scene(&raw_config),
        "inputEnabled": scenario_input_enabled(&raw_config),
    })
}

fn scenario_error(message: impl Into<String>) -> Value {
    json!({ "ok": false, "error": message.into() })
}

/// Full `rumoca-scenario.toml` scenario as a JSON tree for the editor's config GUI:
/// `{ ok, config: <toml-as-json>, descriptor: <scenario_config_response> }`. The
/// `config` tree round-trips every section losslessly (including the nested
/// interactive-IO blocks), so the form can edit known fields and preserve the rest.
pub fn scenario_config_full_to_json(content: &str) -> Value {
    let raw = match toml::from_str::<toml::Value>(content) {
        Ok(raw) => raw,
        Err(error) => return scenario_error(format!("failed to parse scenario config: {error}")),
    };
    let config = serde_json::to_value(&raw).unwrap_or(Value::Null);
    json!({
        "ok": true,
        "config": config,
        "descriptor": scenario_config_response(content),
    })
}

/// Render a `rumoca-scenario.toml` from the GUI's JSON config tree, ensuring the `[rumoca]`
/// marker is present (defaulting version/task). Pure: no filesystem access.
pub fn scenario_config_text_from_json(config: &Value) -> Result<String, String> {
    let mut cleaned = config.clone();
    strip_json_nulls(&mut cleaned);
    let mut toml_value = toml::Value::try_from(&cleaned)
        .map_err(|error| format!("config is not representable as TOML: {error}"))?;
    ensure_rumoca_marker(&mut toml_value);
    toml::to_string_pretty(&toml_value)
        .map_err(|error| format!("failed to serialize scenario config: {error}"))
}

/// TOML has no `null`; drop null-valued keys so the GUI can omit unset fields.
fn strip_json_nulls(value: &mut Value) {
    match value {
        Value::Object(map) => {
            map.retain(|_, entry| !entry.is_null());
            for entry in map.values_mut() {
                strip_json_nulls(entry);
            }
        }
        Value::Array(items) => {
            for entry in items.iter_mut() {
                strip_json_nulls(entry);
            }
        }
        _ => {}
    }
}

fn ensure_rumoca_marker(value: &mut toml::Value) {
    let toml::Value::Table(root) = value else {
        return;
    };
    let marker = root
        .entry("rumoca".to_string())
        .or_insert_with(|| toml::Value::Table(Default::default()));
    if let toml::Value::Table(marker_table) = marker {
        marker_table
            .entry("version".to_string())
            .or_insert_with(|| toml::Value::String(super::RUMOCA_TASK_FILE_VERSION.to_string()));
        marker_table
            .entry("task".to_string())
            .or_insert_with(|| toml::Value::String("simulate".to_string()));
    }
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

fn scenario_input_enabled(raw: &toml::Value) -> bool {
    table_has_key(raw, "input")
}

fn scenario_viewer_mode(config: &ScenarioConfigFile) -> &'static str {
    match config.viewer.mode {
        Some(ScenarioViewerMode::ResultsPanel) => "results_panel",
        Some(ScenarioViewerMode::ExternalWeb) => "external_web",
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
    #[serde(
        default,
        rename = "scatterSeries",
        skip_serializing_if = "Option::is_none"
    )]
    _scatter_series: Option<Vec<Value>>,
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
            _scatter_series: None,
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
