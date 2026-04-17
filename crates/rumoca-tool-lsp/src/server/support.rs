use super::*;
pub(super) use crate::completion_metrics::{CompletionProgressSummary, CompletionTimingSummary};
use rumoca_session::compile::{SessionCacheStatsSnapshot, SourceRootStatusSnapshot};
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
    pub(super) fn accumulate_into(self, timing: &mut ProjectReloadTiming) {
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
    pub(super) reload_project_config_ms: u64,
    pub(super) project_discover_ms: u64,
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
pub(super) struct ProjectReloadTiming {
    pub(super) source_root_paths_changed: bool,
    pub(super) project_discover_ms: u64,
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
    if std::env::var_os("RUMOCA_LSP_COMPLETION_DEBUG").is_none() {
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
    let Some(path) = timing_output_path(explicit_path, "RUMOCA_LSP_COMPLETION_TIMING_FILE") else {
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
    let Some(path) = timing_output_path(explicit_path, "RUMOCA_LSP_COMPLETION_PROGRESS_FILE")
    else {
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
    let Some(path) = timing_output_path(explicit_path, "RUMOCA_LSP_DIAGNOSTICS_TIMING_FILE") else {
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
    let Some(path) = timing_output_path(explicit_path, "RUMOCA_LSP_NAVIGATION_TIMING_FILE") else {
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
    let Some(path) = timing_output_path(explicit_path, "RUMOCA_LSP_STARTUP_TIMING_FILE") else {
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

fn timing_output_path(explicit_path: Option<&Path>, env_var: &str) -> Option<PathBuf> {
    explicit_path
        .map(Path::to_path_buf)
        .or_else(|| std::env::var_os(env_var).map(PathBuf::from))
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

#[derive(Debug, Clone)]
pub(super) struct SimulationRequestSettings {
    pub(super) solver: String,
    pub(super) t_end: f64,
    pub(super) dt: Option<f64>,
    pub(super) source_root_paths: Vec<String>,
}

pub(super) fn simulation_request_settings_from_effective(
    settings: &EffectiveSimulationConfig,
) -> SimulationRequestSettings {
    SimulationRequestSettings {
        solver: settings.solver.clone(),
        t_end: settings.t_end,
        dt: settings.dt,
        source_root_paths: settings.source_root_paths.clone(),
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
    Some(SimulationRequestSettings {
        solver,
        t_end,
        dt,
        source_root_paths,
    })
}

pub(super) fn parse_fallback_simulation(
    value: Option<&Value>,
) -> Option<EffectiveSimulationConfig> {
    let value = value?;
    let obj = value.as_object()?;
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
    Some(EffectiveSimulationConfig {
        solver,
        t_end,
        dt,
        output_dir,
        source_root_paths,
    })
}

pub(super) fn simulation_settings_to_json(settings: &EffectiveSimulationConfig) -> Value {
    json!({
        "solver": settings.solver,
        "tEnd": settings.t_end,
        "dt": settings.dt,
        "outputDir": settings.output_dir,
        "sourceRootPaths": settings.source_root_paths,
    })
}

pub(super) fn find_open_workspace_document_for_model(
    snapshot: &SessionSnapshot,
    model: &str,
) -> Option<Url> {
    for uri in snapshot.document_uris() {
        // Intentionally exclude live overlays on non-workspace source roots here:
        // this helper is selecting a user workspace document to drive project
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

pub(super) fn simulation_preset_to_json(preset: &EffectiveSimulationPreset) -> Value {
    json!({
        "solver": preset.solver,
        "tEnd": preset.t_end,
        "dt": preset.dt,
        "outputDir": preset.output_dir,
        "sourceRootOverrides": preset.source_root_overrides,
    })
}

pub(super) fn simulation_override_from_json(value: &Value) -> Option<SimulationModelOverride> {
    let obj = value.as_object()?;
    let solver = obj
        .get("solver")
        .and_then(Value::as_str)
        .map(str::to_string);
    let t_end = obj.get("tEnd").and_then(Value::as_f64);
    let dt = obj.get("dt").and_then(Value::as_f64);
    let output_dir = obj
        .get("outputDir")
        .and_then(Value::as_str)
        .map(str::to_string);
    let source_root_overrides = obj
        .get("sourceRootOverrides")
        .and_then(Value::as_array)
        .map(|items| {
            items
                .iter()
                .filter_map(Value::as_str)
                .map(str::trim)
                .filter(|value| !value.is_empty())
                .map(str::to_string)
                .collect::<Vec<_>>()
        })
        .unwrap_or_default();
    Some(SimulationModelOverride {
        solver,
        t_end,
        dt,
        output_dir,
        source_root_overrides,
    })
}

pub(super) fn parse_views_payload(value: &Value) -> Option<Vec<PlotViewConfig>> {
    serde_json::from_value::<Vec<VisualizationViewPayload>>(value.clone())
        .ok()
        .map(|views| views.into_iter().map(PlotViewConfig::from).collect())
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub(super) struct VisualizationViewPayload {
    pub(super) id: String,
    pub(super) title: String,
    #[serde(rename = "type")]
    pub(super) view_type: String,
    pub(super) x: Option<String>,
    #[serde(default)]
    pub(super) y: Vec<String>,
    pub(super) script: Option<String>,
    pub(super) script_path: Option<String>,
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
            sources.push((uri, doc.content));
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

pub(super) fn is_project_config_uri(uri: &Url) -> bool {
    if let Ok(path) = uri.to_file_path() {
        return path
            .components()
            .any(|component| component.as_os_str() == ".rumoca");
    }
    uri.path().contains("/.rumoca/")
}

pub(super) fn session_document_uri_key(uri: &Url) -> String {
    if let Ok(path) = uri.to_file_path() {
        return path.to_string_lossy().to_string();
    }
    uri.path().to_string()
}

pub(super) fn parse_file_move_hints(value: Option<&Value>) -> Vec<ProjectFileMoveHint> {
    let Some(Value::Array(items)) = value else {
        return Vec::new();
    };
    let mut hints = Vec::new();
    for item in items {
        let Some(obj) = item.as_object() else {
            continue;
        };
        let old_path = obj
            .get("oldPath")
            .or_else(|| obj.get("old_path"))
            .and_then(Value::as_str)
            .unwrap_or("")
            .trim()
            .to_string();
        let new_path = obj
            .get("newPath")
            .or_else(|| obj.get("new_path"))
            .and_then(Value::as_str)
            .unwrap_or("")
            .trim()
            .to_string();
        if old_path.is_empty() || new_path.is_empty() {
            continue;
        }
        hints.push(ProjectFileMoveHint { old_path, new_path });
    }
    hints
}

pub(super) fn session_uri_path_to_pathbuf(uri_path: &str) -> PathBuf {
    #[cfg(windows)]
    {
        if let Some(rest) = uri_path.strip_prefix('/') {
            let bytes = rest.as_bytes();
            if bytes.len() >= 2 && bytes[0].is_ascii_alphabetic() && bytes[1] == b':' {
                return PathBuf::from(rest);
            }
        }
    }
    PathBuf::from(uri_path)
}

pub(super) fn collect_workspace_known_models_from_session(
    session: &Session,
    workspace_root: &Path,
) -> Vec<String> {
    let mut parsed_docs: Vec<(String, ast::StoredDefinition)> = Vec::new();
    for uri in session.document_uris() {
        let Some(document) = session.get_document(uri) else {
            continue;
        };
        let Some(parsed) = document.parsed() else {
            continue;
        };
        let path = session_uri_path_to_pathbuf(uri);
        if !path.starts_with(workspace_root) || !path.is_file() {
            continue;
        }
        parsed_docs.push((uri.to_string(), parsed.clone()));
    }

    if parsed_docs.is_empty() {
        return Vec::new();
    }

    match merge_stored_definitions(parsed_docs) {
        Ok(merged) => {
            let mut names = collect_model_names(&merged);
            names.sort();
            names.dedup();
            names
        }
        Err(_) => Vec::new(),
    }
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
                "scriptPath": ".rumoca/models/by-id/uuid/viewer_3d.js"
            }
        ]))
        .expect("parse views payload");
        assert_eq!(parsed.len(), 1);
        assert_eq!(
            parsed[0].script_path.as_deref(),
            Some(".rumoca/models/by-id/uuid/viewer_3d.js")
        );
    }

    #[test]
    fn visualization_view_payload_serializes_script_path_as_camel_case() {
        let payload = VisualizationViewPayload::from(PlotViewConfig {
            id: "viewer_3d".to_string(),
            title: "Viewer".to_string(),
            view_type: "3d".to_string(),
            x: None,
            y: Vec::new(),
            script: None,
            script_path: Some(".rumoca/models/by-id/uuid/viewer_3d.js".to_string()),
        });
        let encoded = serde_json::to_value(payload).expect("serialize payload");
        assert_eq!(
            encoded.get("scriptPath").and_then(Value::as_str),
            Some(".rumoca/models/by-id/uuid/viewer_3d.js")
        );
        assert!(encoded.get("script_path").is_none());
    }
}
