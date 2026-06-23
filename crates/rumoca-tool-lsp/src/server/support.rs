use super::*;
pub(super) use crate::completion_metrics::{CompletionProgressSummary, CompletionTimingSummary};
use rumoca_compile::compile::{SessionCacheStatsSnapshot, SourceRootStatusSnapshot};
// Scenario (`rumoca-scenario.toml`) config command JSON shaping is owned by `rumoca-compile`
// so the LSP server and the browser editor bindings share one implementation.
pub(super) use rumoca_compile::scenario::{
    codegen_config_from_json, codegen_config_to_json, parse_fallback_simulation,
    parse_views_payload, scenario_config_full_to_json, scenario_config_response,
    scenario_config_text_from_json, simulation_override_from_json, simulation_preset_to_json,
    simulation_settings_to_json, source_roots_from_json, source_roots_to_json,
    visualization_views_to_json,
};
use std::fs::OpenOptions;
use std::io::Write as _;
use std::path::{Path, PathBuf};

#[derive(Debug, Clone)]
pub(super) struct WorkspaceSymbolTimingBreakdown {
    pub(super) snapshot_ms: u64,
    pub(super) snapshot_lock_ms: u64,
    pub(super) snapshot_build_ms: u64,
    pub(super) snapshot_detail: Option<String>,
    pub(super) query_ms: Option<u64>,
    pub(super) format_ms: Option<u64>,
}

#[derive(Debug, Clone)]
pub(super) struct SourceRootLoadOutcome {
    pub(super) cache_status: SourceRootCacheStatus,
    pub(super) parsed_file_count: usize,
    pub(super) inserted_file_count: usize,
    pub(super) cache_key: String,
    pub(super) cache_path: String,
    pub(super) timing: DurableSourceRootLoadTiming,
    pub(super) status: Option<SourceRootStatusSnapshot>,
}

#[derive(Debug, Clone, Copy, Default)]
pub(super) struct DurableSourceRootLoadTiming {
    pub(super) cache: SourceRootCacheTiming,
    pub(super) apply_ms: u64,
}

impl DurableSourceRootLoadTiming {
    pub(super) fn accumulate_into(self, timing: &mut ScenarioReloadTiming) {
        timing.durable_collect_files_ms += self.cache.collect_files_ms;
        timing.durable_hash_inputs_ms += self.cache.hash_inputs_ms;
        timing.durable_cache_lookup_ms += self.cache.cache_lookup_ms;
        timing.durable_cache_deserialize_ms += self.cache.cache_deserialize_ms;
        timing.durable_parse_files_ms += self.cache.parse_files_ms;
        timing.durable_validate_layout_ms += self.cache.validate_layout_ms;
        timing.durable_cache_write_ms += self.cache.cache_write_ms;
        timing.durable_apply_ms += self.apply_ms;
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) struct AnalysisRequestToken {
    pub(super) mutation_epoch: u64,
    pub(super) session_revision: u64,
}

#[derive(Debug, serde::Serialize)]
#[serde(rename_all = "camelCase")]
pub(super) struct DiagnosticsTimingSummary {
    pub(super) requested_edit_epoch: u64,
    pub(super) request_was_stale: bool,
    pub(super) uri: String,
    pub(super) trigger: &'static str,
    pub(super) semantic_layer: &'static str,
    pub(super) requested_source_root_load: bool,
    pub(super) source_root_load_ms: u64,
    pub(super) ran_compile: bool,
    pub(super) diagnostics_compute_ms: u64,
    pub(super) total_ms: u64,
    pub(super) session_cache_delta: SessionCacheStatsSnapshot,
}

#[derive(Debug, serde::Serialize)]
#[serde(rename_all = "camelCase")]
pub(super) struct StartupTimingSummary {
    pub(super) initial_source_root_paths: usize,
    pub(super) source_root_paths_changed: bool,
    pub(super) parse_init_options_ms: u64,
    pub(super) workspace_root_ms: u64,
    pub(super) reload_scenario_config_ms: u64,
    pub(super) scenario_discover_ms: u64,
    pub(super) resolve_source_root_paths_ms: u64,
    pub(super) reset_session_ms: u64,
    pub(super) durable_prewarm_ms: u64,
    pub(super) durable_collect_files_ms: u64,
    pub(super) durable_hash_inputs_ms: u64,
    pub(super) durable_cache_lookup_ms: u64,
    pub(super) durable_cache_deserialize_ms: u64,
    pub(super) durable_parse_files_ms: u64,
    pub(super) durable_validate_layout_ms: u64,
    pub(super) durable_cache_write_ms: u64,
    pub(super) durable_apply_ms: u64,
    pub(super) workspace_symbol_prewarm_ms: u64,
    pub(super) source_root_read_prewarm_spawn_ms: u64,
    pub(super) total_ms: u64,
}

#[derive(Debug, Clone, Copy, Default)]
pub(super) struct ScenarioReloadTiming {
    pub(super) source_root_paths_changed: bool,
    pub(super) scenario_discover_ms: u64,
    pub(super) resolve_source_root_paths_ms: u64,
    pub(super) reset_session_ms: u64,
    pub(super) durable_prewarm_ms: u64,
    pub(super) durable_collect_files_ms: u64,
    pub(super) durable_hash_inputs_ms: u64,
    pub(super) durable_cache_lookup_ms: u64,
    pub(super) durable_cache_deserialize_ms: u64,
    pub(super) durable_parse_files_ms: u64,
    pub(super) durable_validate_layout_ms: u64,
    pub(super) durable_cache_write_ms: u64,
    pub(super) durable_apply_ms: u64,
    pub(super) workspace_symbol_prewarm_ms: u64,
    pub(super) source_root_read_prewarm_spawn_ms: u64,
    pub(super) total_ms: u64,
}

#[derive(Debug, Clone, Copy)]
pub(super) struct SimulationMetrics {
    pub(super) compile_elapsed: f64,
    pub(super) sim_elapsed: f64,
    pub(super) prepare_context_seconds: f64,
    pub(super) build_snapshot_seconds: f64,
    pub(super) strict_compile_seconds: f64,
    pub(super) strict_resolve_seconds: f64,
    pub(super) instantiate_seconds: f64,
    pub(super) typecheck_seconds: f64,
    pub(super) flatten_seconds: f64,
    pub(super) todae_seconds: f64,
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub(super) struct CodeLensResolutionData {
    pub(super) uri: String,
    pub(super) model_name: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize)]
#[serde(rename_all = "snake_case")]
pub(super) enum NavigationRequestPath {
    QueryOnly,
    FlatPreview,
}

#[derive(Debug, serde::Serialize)]
#[serde(rename_all = "camelCase")]
pub(super) struct NavigationTimingSummary {
    pub(super) requested_edit_epoch: u64,
    pub(super) request_was_stale: bool,
    pub(super) uri: String,
    pub(super) request: &'static str,
    pub(super) request_path: NavigationRequestPath,
    pub(super) semantic_layer: &'static str,
    pub(super) total_ms: u64,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(super) snapshot_ms: Option<u64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(super) snapshot_lock_ms: Option<u64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(super) snapshot_build_ms: Option<u64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(super) detail: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(super) query_ms: Option<u64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(super) format_ms: Option<u64>,
    pub(super) built_resolved_tree: bool,
    pub(super) had_resolved_cache_before: bool,
    pub(super) session_cache_delta: SessionCacheStatsSnapshot,
}

pub(super) async fn maybe_log_completion_debug(client: &Client, message: impl Into<String>) {
    if !tracing::enabled!(target: "rumoca_tool_lsp::completion", tracing::Level::DEBUG) {
        return;
    }
    client
        .log_message(
            MessageType::INFO,
            format!("[rumoca][completion-debug] {}", message.into()),
        )
        .await;
}

pub(super) fn write_completion_timing_summary(
    summary: &CompletionTimingSummary,
    explicit_path: Option<&Path>,
) {
    let Some(path) = timing_output_path(explicit_path) else {
        return;
    };
    let Ok(mut file) = OpenOptions::new().create(true).append(true).open(path) else {
        return;
    };
    let Ok(payload) = serde_json::to_string(summary) else {
        return;
    };
    let _ = writeln!(file, "{payload}");
}

pub(super) fn write_completion_progress_summary(
    summary: &CompletionProgressSummary,
    explicit_path: Option<&Path>,
) {
    let Some(path) = timing_output_path(explicit_path) else {
        return;
    };
    let Ok(mut file) = OpenOptions::new().create(true).append(true).open(path) else {
        return;
    };
    let Ok(payload) = serde_json::to_string(summary) else {
        return;
    };
    let _ = writeln!(file, "{payload}");
}

pub(super) fn write_diagnostics_timing_summary(
    summary: &DiagnosticsTimingSummary,
    explicit_path: Option<&Path>,
) {
    let Some(path) = timing_output_path(explicit_path) else {
        return;
    };
    let Ok(mut file) = OpenOptions::new().create(true).append(true).open(path) else {
        return;
    };
    let Ok(payload) = serde_json::to_string(summary) else {
        return;
    };
    let _ = writeln!(file, "{payload}");
}

pub(super) fn write_navigation_timing_summary(
    summary: &NavigationTimingSummary,
    explicit_path: Option<&Path>,
) {
    let Some(path) = timing_output_path(explicit_path) else {
        return;
    };
    let Ok(mut file) = OpenOptions::new().create(true).append(true).open(path) else {
        return;
    };
    let Ok(payload) = serde_json::to_string(summary) else {
        return;
    };
    let _ = writeln!(file, "{payload}");
}

pub(super) fn write_startup_timing_summary(
    summary: &StartupTimingSummary,
    explicit_path: Option<&Path>,
) {
    let Some(path) = timing_output_path(explicit_path) else {
        return;
    };
    let Ok(mut file) = OpenOptions::new().create(true).append(true).open(path) else {
        return;
    };
    let Ok(payload) = serde_json::to_string(summary) else {
        return;
    };
    let _ = writeln!(file, "{payload}");
}

fn timing_output_path(explicit_path: Option<&Path>) -> Option<PathBuf> {
    explicit_path.map(Path::to_path_buf)
}

pub(super) fn diagnostics_trigger_label(trigger: DiagnosticsTrigger) -> &'static str {
    match trigger {
        DiagnosticsTrigger::Live => "live",
        DiagnosticsTrigger::Save => "save",
    }
}

pub(super) fn diagnostics_semantic_layer_label(
    request_was_stale: bool,
    ran_compile: bool,
    delta: &SessionCacheStatsSnapshot,
) -> &'static str {
    if request_was_stale {
        return "stale";
    }
    if !ran_compile {
        return "parse_only";
    }
    if delta.model_stage_semantic_diagnostics_cache_hits > 0
        || delta.model_stage_semantic_diagnostics_cache_misses > 0
        || delta.model_stage_semantic_diagnostics_builds > 0
    {
        return "model_stage";
    }
    if delta.body_semantic_diagnostics_cache_hits > 0
        || delta.body_semantic_diagnostics_cache_misses > 0
        || delta.body_semantic_diagnostics_builds > 0
    {
        return "body";
    }
    if delta.interface_semantic_diagnostics_cache_hits > 0
        || delta.interface_semantic_diagnostics_cache_misses > 0
        || delta.interface_semantic_diagnostics_builds > 0
    {
        return "interface";
    }
    "parse_only"
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum SourceRootIndexingReason {
    StartupDurablePrewarm,
    CompletionImports,
    SaveDiagnostics,
    SimulationCompile,
}

impl SourceRootIndexingReason {
    pub(super) fn label(self) -> &'static str {
        match self {
            Self::StartupDurablePrewarm => "startup durable source-root warm-start",
            Self::CompletionImports => "editor completion/imports",
            Self::SaveDiagnostics => "save diagnostics",
            Self::SimulationCompile => "simulation compile after source-root edits",
        }
    }

    pub(super) fn stale_label(self) -> String {
        format!("{} (discarded stale result)", self.label())
    }
}

pub(super) fn source_root_load_diagnostics_for_package_layout_error(
    err: &PackageLayoutError,
) -> HashMap<String, Vec<Diagnostic>> {
    let mut by_uri = HashMap::new();
    let source_map = err.source_map();
    for file_name in source_map.source_ids().into_keys() {
        let diagnostics =
            handlers::common_diagnostics_for_file(err.diagnostics(), &file_name, source_map);
        if diagnostics.is_empty() {
            continue;
        }
        by_uri.insert(canonical_path_key(&file_name), diagnostics);
    }
    by_uri
}

pub(super) fn source_root_load_error_message(lib_path: &str, err: &anyhow::Error) -> String {
    let Some(layout) = err.downcast_ref::<PackageLayoutError>() else {
        return format!("Failed to load source root '{}': {}", lib_path, err);
    };
    if layout
        .diagnostics()
        .iter()
        .any(|diagnostic| !diagnostic.labels.is_empty())
    {
        return format!(
            "Failed to load source root '{}': invalid Modelica package layout (see diagnostics)",
            lib_path
        );
    }
    format!("Failed to load source root '{}': {}", lib_path, err)
}

#[derive(Debug, Clone, Default)]
pub(super) struct SimulationRequestSettings {
    pub(super) solver: String,
    pub(super) t_end: f64,
    pub(super) dt: Option<f64>,
    pub(super) source_root_paths: Vec<String>,
    pub(super) parameter_overrides: Vec<(String, f64)>,
}

pub(super) fn simulation_request_settings_from_effective(
    settings: &EffectiveSimulationConfig,
) -> SimulationRequestSettings {
    SimulationRequestSettings {
        solver: settings.solver.clone(),
        t_end: settings.t_end,
        dt: settings.dt,
        source_root_paths: settings.source_root_paths.clone(),
        parameter_overrides: Vec::new(),
    }
}

pub(super) fn parse_simulation_request_settings(
    value: Option<&Value>,
) -> Option<SimulationRequestSettings> {
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
    let source_root_paths = obj
        .get("sourceRootPaths")
        .and_then(Value::as_array)
        .map(|items| {
            items
                .iter()
                .filter_map(Value::as_str)
                .map(str::trim)
                .filter(|v| !v.is_empty())
                .map(str::to_string)
                .collect::<Vec<_>>()
        })
        .unwrap_or_default();
    let parameter_overrides = match obj.get("parameterOverrides") {
        Some(Value::Object(values)) => values
            .iter()
            .filter_map(|(name, value)| {
                value
                    .as_f64()
                    .filter(|v| v.is_finite())
                    .map(|v| (name.clone(), v))
            })
            .collect(),
        Some(_) => return None,
        None => Vec::new(),
    };
    Some(SimulationRequestSettings {
        solver,
        t_end,
        dt,
        source_root_paths,
        parameter_overrides,
    })
}

pub(super) fn find_open_workspace_document_for_model(
    snapshot: &SessionSnapshot,
    model: &str,
) -> Option<Url> {
    for uri in snapshot.document_uris() {
        // Intentionally exclude live overlays on non-workspace source roots here:
        // this helper is selecting a user workspace document to drive scenario
        // command prewarm, not a generic source-root-backed semantic input.
        if snapshot.is_non_workspace_source_root_document(&uri) {
            continue;
        }
        let doc = snapshot.get_document(&uri)?;
        if doc.content.is_empty() {
            continue;
        }
        if collect_model_names(doc.best_effort())
            .iter()
            .any(|candidate| candidate == model)
        {
            return Url::from_file_path(&uri)
                .ok()
                .or_else(|| Url::parse(&uri).ok());
        }
    }
    None
}

pub(super) fn normalize_solver_opt(value: Option<String>) -> Option<String> {
    match value
        .as_deref()
        .map(str::trim)
        .map(str::to_ascii_lowercase)
        .as_deref()
    {
        Some("auto") => Some("auto".to_string()),
        Some("bdf") => Some("bdf".to_string()),
        Some("rk-like") => Some("rk-like".to_string()),
        _ => None,
    }
}

pub(super) fn normalize_dt_opt(value: Option<f64>) -> Option<f64> {
    value.filter(|v| v.is_finite() && *v > 0.0)
}

#[cfg(test)]
fn simulation_doc_for_compile_impl(
    is_source_root_document: bool,
    uri: &str,
    doc: &Document,
    focus_key: &str,
) -> std::result::Result<Option<(bool, ast::StoredDefinition)>, String> {
    let is_focus_document = canonical_path_key(uri) == focus_key;
    if !is_focus_document && !is_source_root_document {
        return Ok(None);
    }
    let parsed = if is_focus_document {
        doc.recovered().cloned().or_else(|| doc.parsed().cloned())
    } else {
        doc.parsed().cloned()
    };
    match parsed {
        Some(parsed) => Ok(Some((is_focus_document, parsed))),
        None if is_focus_document => Err(format!(
            "active document has no parsed or recovered AST: {}",
            doc.uri
        )),
        None if is_source_root_document => Err(format!(
            "source-root document has no parsed AST: {}",
            doc.uri
        )),
        None => Ok(None),
    }
}

#[cfg(test)]
pub(super) fn simulation_doc_for_compile_snapshot(
    snapshot: &SessionSnapshot,
    uri: &str,
    doc: &Document,
    focus_key: &str,
) -> std::result::Result<Option<(bool, ast::StoredDefinition)>, String> {
    simulation_doc_for_compile_impl(
        snapshot.is_source_root_backed_document(uri),
        uri,
        doc,
        focus_key,
    )
}

fn collect_local_compile_unit_sources_with_lookup(
    focus_document_path: &str,
    mut get_document: impl FnMut(&str) -> Option<Document>,
) -> std::result::Result<Vec<(String, String)>, String> {
    let paths = collect_compile_unit_source_files(Path::new(focus_document_path))
        .map_err(|err| format!("failed to collect local compile unit: {err}"))?;
    let mut sources = Vec::new();

    for path in paths {
        let uri = path.to_string_lossy().to_string();
        if let Some(doc) = get_document(&uri)
            && !doc.content.is_empty()
        {
            sources.push((uri, doc.content.to_string()));
            continue;
        }

        let source = std::fs::read_to_string(&path).map_err(|err| {
            format!(
                "failed to read local compile unit document '{}': {}",
                uri, err
            )
        })?;
        sources.push((uri, source));
    }

    Ok(sources)
}

#[cfg(test)]
pub(super) fn collect_local_compile_unit_sources_snapshot(
    snapshot: &SessionSnapshot,
    focus_document_path: &str,
) -> std::result::Result<Vec<(String, String)>, String> {
    collect_local_compile_unit_sources_with_lookup(focus_document_path, |uri| {
        snapshot.get_document(uri)
    })
}

pub(super) fn collect_local_compile_unit_sources_session(
    session: &Session,
    focus_document_path: &str,
) -> std::result::Result<Vec<(String, String)>, String> {
    collect_local_compile_unit_sources_with_lookup(focus_document_path, |uri| {
        session.get_document(uri).cloned()
    })
}

#[cfg(test)]
pub(super) fn collect_simulation_parsed_docs_snapshot(
    snapshot: &SessionSnapshot,
    focus_document_path: &str,
    focus_key: &str,
) -> std::result::Result<Vec<(String, ast::StoredDefinition)>, String> {
    let uris = snapshot.document_uris();
    let mut has_focus_document = false;
    let mut parsed_docs = Vec::new();

    for uri in uris {
        let Some(doc) = snapshot.get_document(&uri) else {
            continue;
        };
        let Some((is_focus_document, parsed)) =
            simulation_doc_for_compile_snapshot(snapshot, &uri, &doc, focus_key)?
        else {
            continue;
        };
        has_focus_document |= is_focus_document;
        parsed_docs.push((doc.uri.clone(), parsed));
    }

    if !has_focus_document {
        return Err(format!(
            "active document not found in session: {focus_document_path}"
        ));
    }
    Ok(parsed_docs)
}

pub(super) fn is_scenario_config_uri(uri: &Url) -> bool {
    // Rumoca task-file naming convention (`rumoca-scenario.toml` / `rumoca-scenario.<profile>.toml`) is
    // the discovery hook; the `[rumoca]` marker section stays authoritative.
    let matches = rumoca_compile::scenario::is_rumoca_task_filename;
    if let Ok(path) = uri.to_file_path() {
        return path
            .file_name()
            .and_then(|name| name.to_str())
            .is_some_and(matches);
    }
    uri.path().rsplit('/').next().is_some_and(matches)
}

pub(super) fn session_document_uri_key(uri: &Url) -> String {
    if let Ok(path) = uri.to_file_path() {
        return path.to_string_lossy().to_string();
    }
    uri.path().to_string()
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn parse_views_payload_accepts_canonical_camel_case_only() {
        let parsed = parse_views_payload(&json!([
            {
                "id": "viewer_3d",
                "title": "Viewer",
                "type": "3d",
                "y": [],
                "scriptPath": "viewer_3d.js"
            }
        ]))
        .expect("parse views payload");
        assert_eq!(parsed.len(), 1);
        assert_eq!(parsed[0].script_path.as_deref(), Some("viewer_3d.js"));
    }
}
