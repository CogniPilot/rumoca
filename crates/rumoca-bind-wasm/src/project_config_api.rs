//! Project (`rum.toml`) configuration commands for the browser editor.
//!
//! These mirror the LSP server's `rumoca.project.*` config commands but operate
//! on the editor's in-memory project files (the `projectSources` JSON map of
//! `path -> content`) instead of a real filesystem. The actual logic and JSON
//! shaping live in `rumoca_compile::project`, shared with the LSP server, so the
//! browser editor and VS Code stay in lockstep.

use std::collections::BTreeMap;
use std::path::{Path, PathBuf};

use rumoca_compile::project::{
    ProjectConfig, parse_fallback_simulation, parse_views_payload, scenario_config_response,
    simulation_override_from_json, simulation_preset_to_json, simulation_settings_to_json,
    visualization_views_to_json,
};
use serde_json::{Value, json};
use wasm_bindgen::JsValue;

// Editor project paths are workspace-relative, so an empty workspace root keeps
// any newly created `rum.<model>.toml` write path relative too.
fn project_config_from_sources(project_sources_json: &str) -> Result<ProjectConfig, JsValue> {
    Ok(ProjectConfig::from_files(
        Path::new(""),
        parse_project_files(project_sources_json)?,
    ))
}

fn parse_project_files(project_sources_json: &str) -> Result<Vec<(PathBuf, String)>, JsValue> {
    let trimmed = project_sources_json.trim();
    if trimmed.is_empty() {
        return Ok(Vec::new());
    }
    let map: BTreeMap<String, String> = serde_json::from_str(trimmed)
        .map_err(|e| JsValue::from_str(&format!("Invalid project sources JSON: {e}")))?;
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
/// single model's config file, applied by the editor to its project store.
fn writes_response(config: &ProjectConfig, model: &str) -> Result<String, JsValue> {
    let mut writes = Vec::new();
    if let Some(result) = config.compute_write_for_model(model) {
        let (path, content) =
            result.map_err(|e| JsValue::from_str(&format!("render config error: {e}")))?;
        writes.push(json!({ "path": path.to_string_lossy(), "content": content }));
    }
    to_json_string(&json!({ "writes": writes, "result": { "ok": true } }))
}

pub(crate) fn get_simulation_config_impl(
    project_sources_json: &str,
    model: &str,
    fallback_json: &str,
) -> Result<String, JsValue> {
    let config = project_config_from_sources(project_sources_json)?;
    let fallback = parse_optional_json(fallback_json)
        .as_ref()
        .and_then(|value| parse_fallback_simulation(Some(value)))
        .unwrap_or_default();
    let snapshot = config.simulation_snapshot_for_model(model, &fallback);
    to_json_string(&json!({
        "preset": snapshot.preset.as_ref().map(simulation_preset_to_json),
        "defaults": simulation_settings_to_json(&snapshot.defaults),
        "effective": simulation_settings_to_json(&snapshot.effective),
        "diagnostics": snapshot.diagnostics,
    }))
}

pub(crate) fn set_simulation_preset_impl(
    project_sources_json: &str,
    model: &str,
    preset_json: &str,
) -> Result<String, JsValue> {
    let mut config = project_config_from_sources(project_sources_json)?;
    let model_override = parse_optional_json(preset_json)
        .as_ref()
        .and_then(simulation_override_from_json)
        .ok_or_else(|| JsValue::from_str("invalid simulation preset payload"))?;
    config.set_model_simulation_preset(model, model_override);
    writes_response(&config, model)
}

pub(crate) fn reset_simulation_preset_impl(
    project_sources_json: &str,
    model: &str,
) -> Result<String, JsValue> {
    let mut config = project_config_from_sources(project_sources_json)?;
    config.remove_model_override(model);
    writes_response(&config, model)
}

pub(crate) fn get_visualization_config_impl(
    project_sources_json: &str,
    model: &str,
) -> Result<String, JsValue> {
    let config = project_config_from_sources(project_sources_json)?;
    to_json_string(&visualization_views_to_json(
        config.plot_views_for_model(model),
    ))
}

pub(crate) fn set_visualization_config_impl(
    project_sources_json: &str,
    model: &str,
    views_json: &str,
) -> Result<String, JsValue> {
    let mut config = project_config_from_sources(project_sources_json)?;
    let views = parse_optional_json(views_json)
        .as_ref()
        .and_then(parse_views_payload)
        .ok_or_else(|| JsValue::from_str("invalid visualization views payload"))?;
    config.set_plot_views(model, views);
    writes_response(&config, model)
}

pub(crate) fn get_scenario_config_impl(
    project_sources_json: &str,
    path: &str,
) -> Result<String, JsValue> {
    let files = parse_project_files(project_sources_json)?;
    let needle = path.trim();
    let content = files
        .iter()
        .find(|(file_path, _)| {
            let candidate = file_path.to_string_lossy();
            candidate == needle || (!needle.is_empty() && candidate.ends_with(needle))
        })
        .map(|(_, content)| content.clone());
    match content {
        Some(content) => to_json_string(&scenario_config_response(&content)),
        None => to_json_string(&json!({
            "ok": false,
            "error": "rum.toml scenario was not found in the project",
        })),
    }
}
