use std::path::{Path, PathBuf};

use indexmap::IndexMap;
use rumoca_compile::workspace::{WorkspaceConfig, is_workspace_config_filename};
use wasm_bindgen::prelude::*;

fn parse_workspace_config_files(
    workspace_sources_json: &str,
) -> Result<Vec<(PathBuf, String)>, JsValue> {
    let trimmed = workspace_sources_json.trim();
    if trimmed.is_empty() {
        return Ok(Vec::new());
    }
    let map: IndexMap<String, String> = serde_json::from_str(trimmed)
        .map_err(|e| JsValue::from_str(&format!("Invalid workspace sources JSON: {e}")))?;
    Ok(map
        .into_iter()
        .filter(|(path, _)| {
            Path::new(path)
                .file_name()
                .and_then(|name| name.to_str())
                .is_some_and(is_workspace_config_filename)
        })
        .map(|(path, content)| (PathBuf::from(path), content))
        .collect())
}

#[wasm_bindgen]
pub fn workspace_effective_source_roots(
    workspace_sources_json: &str,
    focus_path: &str,
) -> Result<String, JsValue> {
    let focus_path = Path::new(focus_path.trim());
    let config = WorkspaceConfig::load_from_files(
        Path::new(""),
        focus_path,
        parse_workspace_config_files(workspace_sources_json)?,
    )
    .map_err(|e| JsValue::from_str(&format!("Workspace config error: {e}")))?;
    let roots = config.effective_source_roots_for(focus_path);
    serde_json::to_string(&roots).map_err(|e| JsValue::from_str(&format!("JSON error: {e}")))
}
