//! Scenario (`rumoca-scenario.toml`) configuration commands for the browser editor.
//!
//! These mirror the LSP server's `rumoca.scenario.*` config commands but operate
//! on the editor's in-memory workspace source map (`path -> content`) instead
//! of a real filesystem. The actual logic and JSON
//! shaping live in `rumoca_compile::scenario`, shared with the LSP server, so the
//! browser editor and VS Code stay in lockstep.

use std::path::{Path, PathBuf};

use indexmap::IndexMap;
use rumoca_compile::scenario::{
    ScenarioConfig, ScenarioTask, codegen_config_from_json, codegen_config_to_json,
    parse_fallback_simulation, parse_views_payload, scenario_config_full_to_json,
    scenario_config_response, scenario_config_text_from_json, simulation_override_from_json,
    simulation_preset_to_json, simulation_settings_to_json, source_roots_from_json,
    source_roots_to_json, visualization_views_to_json,
};
use serde_json::{Value, json};
use wasm_bindgen::JsValue;

// Editor paths are workspace-relative, so an empty workspace root keeps
// any newly created `rumoca-scenario.<model>.toml` write path relative too.
fn scenario_config_from_sources(workspace_sources_json: &str) -> Result<ScenarioConfig, JsValue> {
    Ok(ScenarioConfig::from_files(
        Path::new(""),
        parse_workspace_files(workspace_sources_json)?,
    ))
}

fn parse_workspace_files(workspace_sources_json: &str) -> Result<Vec<(PathBuf, String)>, JsValue> {
    let trimmed = workspace_sources_json.trim();
    if trimmed.is_empty() {
        return Ok(Vec::new());
    }
    let map: IndexMap<String, String> = serde_json::from_str(trimmed)
        .map_err(|e| JsValue::from_str(&format!("Invalid workspace sources JSON: {e}")))?;
    Ok(map
        .into_iter()
        .map(|(path, content)| (PathBuf::from(path), content))
        .collect())
}

fn parse_optional_json(raw: &str) -> Option<Value> {
    let trimmed = raw.trim();
    if trimmed.is_empty() || trimmed == "null" {
        return None;
    }
    serde_json::from_str(trimmed).ok()
}

fn to_json_string(value: &Value) -> Result<String, JsValue> {
    serde_json::to_string(value).map_err(|e| JsValue::from_str(&format!("JSON error: {e}")))
}

/// Build a `{ writes: [{path, content}], result: { ok: true } }` response for a
/// single model's config file, applied by the editor to its workspace store.
fn writes_response(config: &ScenarioConfig, model: &str) -> Result<String, JsValue> {
    writes_response_for_task(config, model, ScenarioTask::Simulate)
}

fn writes_response_for_task(
    config: &ScenarioConfig,
    model: &str,
    task: ScenarioTask,
) -> Result<String, JsValue> {
    let mut writes = Vec::new();
    if let Some(result) = config.compute_write_for_model_task(model, task) {
        let (path, content) =
            result.map_err(|e| JsValue::from_str(&format!("render config error: {e}")))?;
        writes.push(json!({ "path": path.to_string_lossy(), "content": content }));
    }
    to_json_string(&json!({ "writes": writes, "result": { "ok": true } }))
}

pub(crate) fn get_simulation_config_impl(
    workspace_sources_json: &str,
    model: &str,
    fallback_json: &str,
) -> Result<String, JsValue> {
    let config = scenario_config_from_sources(workspace_sources_json)?;
    let fallback = parse_optional_json(fallback_json)
        .as_ref()
        .and_then(|value| parse_fallback_simulation(Some(value)))
        .unwrap_or_default();
    let snapshot = config.simulation_snapshot_for_model(model, &fallback);
    to_json_string(&json!({
        "preset": snapshot.preset.as_ref().map(simulation_preset_to_json),
        "defaults": simulation_settings_to_json(&snapshot.defaults),
        "effective": simulation_settings_to_json(&snapshot.effective),
        "scenarioPath": snapshot.scenario_path,
        "diagnostics": snapshot.diagnostics,
    }))
}

pub(crate) fn set_simulation_preset_impl(
    workspace_sources_json: &str,
    model: &str,
    preset_json: &str,
) -> Result<String, JsValue> {
    let mut config = scenario_config_from_sources(workspace_sources_json)?;
    let model_override = parse_optional_json(preset_json)
        .as_ref()
        .and_then(simulation_override_from_json)
        .ok_or_else(|| JsValue::from_str("invalid simulation preset payload"))?;
    config.set_model_simulation_preset(model, model_override);
    writes_response(&config, model)
}

pub(crate) fn reset_simulation_preset_impl(
    workspace_sources_json: &str,
    model: &str,
) -> Result<String, JsValue> {
    let mut config = scenario_config_from_sources(workspace_sources_json)?;
    config.remove_model_override(model);
    writes_response(&config, model)
}

pub(crate) fn get_visualization_config_impl(
    workspace_sources_json: &str,
    model: &str,
) -> Result<String, JsValue> {
    let config = scenario_config_from_sources(workspace_sources_json)?;
    to_json_string(&visualization_views_to_json(
        config.plot_views_for_model(model),
    ))
}

pub(crate) fn set_visualization_config_impl(
    workspace_sources_json: &str,
    model: &str,
    views_json: &str,
) -> Result<String, JsValue> {
    let mut config = scenario_config_from_sources(workspace_sources_json)?;
    let views = parse_optional_json(views_json)
        .as_ref()
        .and_then(parse_views_payload)
        .ok_or_else(|| JsValue::from_str("invalid visualization views payload"))?;
    config.set_plot_views(model, views);
    writes_response(&config, model)
}

pub(crate) fn get_codegen_config_impl(
    workspace_sources_json: &str,
    model: &str,
) -> Result<String, JsValue> {
    let config = scenario_config_from_sources(workspace_sources_json)?;
    to_json_string(&codegen_config_to_json(
        &config.codegen_config_for_model(model),
    ))
}

pub(crate) fn set_codegen_config_impl(
    workspace_sources_json: &str,
    model: &str,
    codegen_json: &str,
) -> Result<String, JsValue> {
    let mut config = scenario_config_from_sources(workspace_sources_json)?;
    let codegen = parse_optional_json(codegen_json)
        .as_ref()
        .and_then(codegen_config_from_json)
        .ok_or_else(|| JsValue::from_str("invalid codegen config payload"))?;
    config.set_codegen_config(model, codegen);
    writes_response_for_task(&config, model, ScenarioTask::Codegen)
}

pub(crate) fn get_source_roots_impl(
    workspace_sources_json: &str,
    model: &str,
    task: &str,
) -> Result<String, JsValue> {
    let config = scenario_config_from_sources(workspace_sources_json)?;
    let task = scenario_task_from_str(task).unwrap_or_default();
    to_json_string(&source_roots_to_json(
        &config.source_roots_for_model_task(model, task),
    ))
}

pub(crate) fn set_source_roots_impl(
    workspace_sources_json: &str,
    model: &str,
    source_roots_json: &str,
) -> Result<String, JsValue> {
    let mut config = scenario_config_from_sources(workspace_sources_json)?;
    let payload = parse_optional_json(source_roots_json);
    let task = payload
        .as_ref()
        .and_then(|value| scenario_task_from_json(value.get("task")))
        .unwrap_or_default();
    let source_roots = payload
        .as_ref()
        .and_then(source_roots_from_json)
        .ok_or_else(|| JsValue::from_str("invalid source roots payload"))?;
    config.set_source_roots_for_task(model, task, source_roots);
    writes_response_for_task(&config, model, task)
}

fn scenario_task_from_json(value: Option<&Value>) -> Option<ScenarioTask> {
    scenario_task_from_str(value.and_then(Value::as_str).unwrap_or_default())
}

fn scenario_task_from_str(value: &str) -> Option<ScenarioTask> {
    match value {
        "simulate" => Some(ScenarioTask::Simulate),
        "codegen" => Some(ScenarioTask::Codegen),
        _ => None,
    }
}

pub(crate) fn get_scenario_config_impl(
    workspace_sources_json: &str,
    path: &str,
) -> Result<String, JsValue> {
    let files = parse_workspace_files(workspace_sources_json)?;
    match find_workspace_file(&files, path.trim()) {
        Some(content) => to_json_string(&scenario_config_response(content)),
        None => to_json_string(&json!({
            "ok": false,
            "error": "rumoca-scenario.toml scenario was not found in the workspace",
        })),
    }
}

fn find_workspace_file<'a>(files: &'a [(PathBuf, String)], needle: &str) -> Option<&'a str> {
    files
        .iter()
        .find(|(file_path, _)| {
            let candidate = file_path.to_string_lossy();
            candidate == needle || (!needle.is_empty() && candidate.ends_with(needle))
        })
        .map(|(_, content)| content.as_str())
}

pub(crate) fn get_scenario_config_full_impl(
    workspace_sources_json: &str,
    path: &str,
) -> Result<String, JsValue> {
    let files = parse_workspace_files(workspace_sources_json)?;
    match find_workspace_file(&files, path.trim()) {
        Some(content) => to_json_string(&scenario_config_full_to_json(content)),
        None => to_json_string(&json!({
            "ok": false,
            "error": "rumoca-scenario.toml scenario was not found in the workspace",
        })),
    }
}

pub(crate) fn set_scenario_config_impl(path: &str, config_json: &str) -> Result<String, JsValue> {
    let config: Value = serde_json::from_str(config_json.trim())
        .map_err(|e| JsValue::from_str(&format!("Invalid scenario config JSON: {e}")))?;
    let content = scenario_config_text_from_json(&config).map_err(|e| JsValue::from_str(&e))?;
    to_json_string(&json!({
        "writes": [{ "path": path, "content": content }],
        "result": { "ok": true },
    }))
}

pub(crate) fn default_scenario_config_impl(
    workspace_sources_json: &str,
    model: &str,
    task: &str,
) -> Result<String, JsValue> {
    let config = scenario_config_from_sources(workspace_sources_json)?;
    let task = scenario_task_from_str(task).unwrap_or_default();
    let (path, content) = config
        .default_scenario_config_for_task(model, task)
        .map_err(|e| JsValue::from_str(&format!("default scenario config error: {e}")))?;
    to_json_string(&json!({
        "ok": true,
        "path": path.to_string_lossy(),
        "content": content,
    }))
}
